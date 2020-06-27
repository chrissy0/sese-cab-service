package de.tuberlin.sese.cabservice.persistence.customerinteraction.pickup;

import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationEntity;
import de.tuberlin.sese.cabservice.persistence.cab.location.CabLocationService;
import de.tuberlin.sese.cabservice.persistence.cab.registration.CabRepo;
import de.tuberlin.sese.cabservice.persistence.cab.registration.persistence.CabEntity;
import de.tuberlin.sese.cabservice.persistence.job.JobEntity;
import de.tuberlin.sese.cabservice.persistence.job.JobService;
import de.tuberlin.sese.cabservice.util.exceptions.CabCustomerPositionConflictException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabIdException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownCabLocationException;
import de.tuberlin.sese.cabservice.util.exceptions.UnknownJobIdException;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit4.SpringRunner;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

import static de.tuberlin.sese.cabservice.persistence.job.JobEntity.CustomerState.IN_CAB;
import static de.tuberlin.sese.cabservice.persistence.job.JobEntity.CustomerState.WAITING;
import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.*;

@RunWith(SpringRunner.class)
@SpringBootTest
public class PickupServiceTest {


    @Autowired
    private PickupService pickupService;

    @MockBean
    private PickupRepo pickupRepo;

    @MockBean
    private CabRepo cabRepo;

    @MockBean
    private CabLocationService locationService;

    @MockBean
    private JobService jobService;

    @Captor
    private ArgumentCaptor<PickupRequestEntity> pickupRequestCaptor;

    @Captor
    private ArgumentCaptor<JobEntity> jobCaptor;

    @Test
    public void shouldPickupCustomer() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(2)
                .build()));

        when(jobService.getJob(3L)).thenReturn(Optional.of(JobEntity.builder()
                .id(3L)
                .start(2)
                .end(5)
                .timestamp(LocalDateTime.of(2020, 6, 27, 7, 48))
                .customerId(3L)
                .customerState(WAITING)
                .inProgress(true)
                .build()));

        pickupService.pickup(1L, 3L);

        verify(cabRepo).findById(1L);
        verify(locationService).getCabLocation(1L);
        verify(jobService).getJob(3L);
        verify(pickupRepo).save(pickupRequestCaptor.capture());

        PickupRequestEntity pickupCaptorValue = pickupRequestCaptor.getValue();
        assertThat(pickupCaptorValue.getCabId()).isEqualTo(1L);
        assertThat(pickupCaptorValue.getCustomerId()).isEqualTo(3L);

        verifyNoMoreInteractions(cabRepo);
        verifyNoMoreInteractions(locationService);
        verifyNoMoreInteractions(jobService);
        verifyNoMoreInteractions(pickupRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCabIdDuringPickup() {
        assertThatThrownBy(() -> pickupService.pickup(null, 3L))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCustomerIdDuringPickup() {
        assertThatThrownBy(() -> pickupService.pickup(1L, null))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldThrowUnknownCabIdExceptionOnUnknownCabIdDuringPickup() {
        when(cabRepo.findById(1L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> pickupService.pickup(1L, 3L))
                .isExactlyInstanceOf(UnknownCabIdException.class);
    }

    @Test
    public void shouldThrowUnknownCabLocationExceptionOnUnknownCabLocationDuringPickup() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> pickupService.pickup(1L, 3L))
                .isExactlyInstanceOf(UnknownCabLocationException.class);
    }

    @Test
    public void shouldThrowUnknownJobIdExceptionOnUnknownJobIdDuringPickup() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(2)
                .build()));

        when(jobService.getJob(3L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> pickupService.pickup(1L, 3L))
                .isExactlyInstanceOf(UnknownJobIdException.class);
    }

    @Test
    public void shouldThrowCabCustomerPositionConflictExceptionWhenCabAndCustomerAtDifferentPositionDuringPickup() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(2)
                .build()));

        when(jobService.getJob(3L)).thenReturn(Optional.of(JobEntity.builder()
                .id(3L)
                .start(5)
                .end(11)
                .timestamp(LocalDateTime.of(2020, 6, 27, 7, 48))
                .customerId(3L)
                .customerState(WAITING)
                .inProgress(true)
                .build()));

        assertThatThrownBy(() -> pickupService.pickup(1L, 3L))
                .isExactlyInstanceOf(CabCustomerPositionConflictException.class);
    }

    @Test
    public void shouldThrowCabCustomerPositionConflictExceptionWhenCustomerWasAlreadyPickedUpDuringPickup() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .name("Some Cab Name")
                .id(1L)
                .build()));

        when(locationService.getCabLocation(1L)).thenReturn(Optional.of(CabLocationEntity.builder()
                .cabId(1L)
                .section(2)
                .build()));

        when(jobService.getJob(3L)).thenReturn(Optional.of(JobEntity.builder()
                .id(3L)
                .start(2)
                .end(5)
                .timestamp(LocalDateTime.of(2020, 6, 27, 7, 48))
                .customerId(3L)
                .customerState(IN_CAB)
                .inProgress(true)
                .build()));

        assertThatThrownBy(() -> pickupService.pickup(1L, 3L))
                .isExactlyInstanceOf(CabCustomerPositionConflictException.class);
    }

    @Test
    public void shouldReturnListWhenGettingPickupRequests() {
        when(pickupRepo.findAll()).thenReturn(asList(
                PickupRequestEntity.builder()
                        .cabId(1L)
                        .customerId(2L)
                        .build(),
                PickupRequestEntity.builder()
                        .cabId(2L)
                        .customerId(1L)
                        .build()));

        List<PickupRequestEntity> pickupRequests = pickupService.getPickupRequests();

        assertThat(pickupRequests).hasSize(2);
        assertThat(pickupRequests).isInstanceOf(List.class);
    }

    @Test
    public void shouldReturnCompleteOnNoActivePickupsDuringPickupsComplete() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .id(1L)
                .name("Some Name")
                .build()));

        when(pickupRepo.findAll()).thenReturn(asList(
                PickupRequestEntity.builder()
                        .cabId(2L)
                        .customerId(1L)
                        .build(),
                PickupRequestEntity.builder()
                        .cabId(3L)
                        .customerId(2L)
                        .build()));

        PickupCompleteModel pickupCompleteModel = pickupService.pickupsComplete(1L);

        assertThat(pickupCompleteModel).isNotNull();
        assertThat(pickupCompleteModel.isComplete()).isTrue();

        verify(cabRepo).findById(1L);
        verify(pickupRepo).findAll();

        verifyNoMoreInteractions(cabRepo);
        verifyNoMoreInteractions(pickupRepo);
    }

    @Test
    public void shouldReturnNotCompleteOnOnlyActivePickupsDuringPickupsComplete() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .id(1L)
                .name("Some Name")
                .build()));

        when(pickupRepo.findAll()).thenReturn(asList(
                PickupRequestEntity.builder()
                        .cabId(1L)
                        .customerId(1L)
                        .build(),
                PickupRequestEntity.builder()
                        .cabId(1L)
                        .customerId(2L)
                        .build()));

        PickupCompleteModel pickupCompleteModel = pickupService.pickupsComplete(1L);

        assertThat(pickupCompleteModel).isNotNull();
        assertThat(pickupCompleteModel.isComplete()).isFalse();

        verify(cabRepo).findById(1L);
        verify(pickupRepo).findAll();

        verifyNoMoreInteractions(cabRepo);
        verifyNoMoreInteractions(pickupRepo);
    }

    @Test
    public void shouldReturnNotCompleteOnSomeActivePickupsDuringPickupsComplete() {
        when(cabRepo.findById(1L)).thenReturn(Optional.of(CabEntity.builder()
                .id(1L)
                .name("Some Name")
                .build()));

        when(pickupRepo.findAll()).thenReturn(asList(
                PickupRequestEntity.builder()
                        .cabId(1L)
                        .customerId(1L)
                        .build(),
                PickupRequestEntity.builder()
                        .cabId(2L)
                        .customerId(2L)
                        .build(),
                PickupRequestEntity.builder()
                        .cabId(3L)
                        .customerId(4L)
                        .build()));

        PickupCompleteModel pickupCompleteModel = pickupService.pickupsComplete(1L);

        assertThat(pickupCompleteModel).isNotNull();
        assertThat(pickupCompleteModel.isComplete()).isFalse();

        verify(cabRepo).findById(1L);
        verify(pickupRepo).findAll();

        verifyNoMoreInteractions(cabRepo);
        verifyNoMoreInteractions(pickupRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionWhenCabIdIsMissingOnPickupsComplete() {
        assertThatThrownBy(() -> pickupService.pickupsComplete(null))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldThrowUnknownCabIdExceptionWhenCabIdIsUnknownOnPickupsComplete() {
        when(cabRepo.findById(1L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> pickupService.pickupsComplete(1L))
                .isExactlyInstanceOf(UnknownCabIdException.class);
    }

    @Test
    public void shouldAcceptPickup() {
        when(jobService.getJob(1L)).thenReturn(Optional.of(JobEntity.builder()
                .id(1L)
                .customerState(WAITING)
                .build()));

        pickupService.acceptPickup(1L);

        verify(jobService).getJob(1L);
        verify(jobService).updateJob(jobCaptor.capture());
        assertThat(jobCaptor.getValue().getCustomerState()).isEqualTo(IN_CAB);
        verify(pickupRepo).deleteById(1L);

        verifyNoMoreInteractions(jobService);
        verifyNoMoreInteractions(pickupRepo);
    }

    @Test
    public void shouldThrowIllegalArgumentExceptionOnMissingCustomerIdDuringAcceptPickup() {
        assertThatThrownBy(() -> pickupService.acceptPickup(null))
                .isExactlyInstanceOf(IllegalArgumentException.class);
    }

    @Test
    public void shouldThrowUnknownJobIdExceptionOnUnknownJobIdDuringAcceptPickup() {
        when(jobService.getJob(1L)).thenReturn(Optional.empty());

        assertThatThrownBy(() -> pickupService.acceptPickup(1L))
                .isExactlyInstanceOf(UnknownJobIdException.class);
    }
}
